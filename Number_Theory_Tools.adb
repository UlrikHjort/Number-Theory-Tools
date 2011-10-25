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
with Ada.Numerics.Elementary_Functions;
with Ada.Numerics.Discrete_Random; use Ada.Numerics;
with Ada.Calendar; use Ada.Calendar;

package body Number_Theory_Tools is

   ---------------------------------------------------------------------------
   --
   -- Returns a random number in the range First .. Last
   --
   ---------------------------------------------------------------------------
   function Get_Random_Large_Positive(First : Large_Positive; Last : Large_Positive)
                                     return Large_Positive is

      subtype Random_Interval is Large_Positive range First ..Last;
      package Random_Large_Positive is new Discrete_Random(Random_Interval);
      use Random_Large_Positive;

      G : Generator;

   begin
      return Random(G);
   end Get_Random_Large_Positive;


   ---------------------------------------------------------------------------
   --
   -- Returns a modified second for use to get some more randomness in the
   -- Random function
   --
   ---------------------------------------------------------------------------
   function Get_A_Second(Random_Val : Large_Positive) return Large_Positive is
      Year,Month,Day : Integer;
      Seconds        : Day_Duration;
      Time_And_Date  : constant Time := Clock;

   begin
      Split(Time_And_Date, Year, Month, Day, Seconds);
      return (Random_Val mod Large_Positive((Integer(Seconds) mod 60) +1)+1);
   end Get_A_Second;


   ---------------------------------------------------------------------------
   --
   -- Returns the sum of the elements of the given Knapsack
   --
   ---------------------------------------------------------------------------
   function Knapsack_Sum(Knapsack : in Large_Positive_Array_T) return Large_Positive is
      Sum : Large_Positive := 0;

   begin
      for Index in Knapsack'First .. Knapsack'Last loop
         Sum := Sum + Knapsack(Index);
      end loop;

      return Sum;
   end Knapsack_Sum;


   ---------------------------------------------------------------------------
   --
   -- Generate and "Easy" Knapsack and returns it in the "Knapsack" buffer
   --
   ---------------------------------------------------------------------------
   procedure Generate_Easy_Knapsack(Knapsack : in out Large_Positive_Array_T) is

      subtype Random_Interval is Large_Positive range 1 ..Large_Positive'Last;
      package Random_Large_Positive is new Discrete_Random(Random_Interval);
      use Random_Large_Positive;

      G : Generator;

      -- Get a start value which is not too big but still created
      -- with some randomness:
      Random_Value : constant Large_Positive := Get_A_Second(Random(G));
      Value        : Large_Positive          := Get_Random_Large_Positive(Random_Value, Random_Value * 4);

      Sum : Large_Positive := Value;
   begin
      for Index in Knapsack'First .. Knapsack'Last loop
         Knapsack(Index) := Value;
         Value := Sum + ((Random(G) mod ((Sum/4)+1)+1));
         Sum := Sum + Value;
      end loop;
   end Generate_Easy_Knapsack;


   ---------------------------------------------------------------------------
   --
   -- Returns a coprime (the smallest) to A;
   --
   ---------------------------------------------------------------------------
  function Get_CoPrime(A : Large_Positive) return Large_Positive is
  begin
     for I in 2 .. A-1 loop
        if Gcd(A,I) = 1 then
           return I;
        end if;
     end loop;
     return 1;
  end Get_CoPrime;


   --------------------------------------------------------------------------------------
   --
   -- Modulo function for real values
   --
   --------------------------------------------------------------------------------------
   function F_Mod (Numerator :in Long_Long_Float; Denominator : in Long_Long_Float) return Long_Long_Float  is

      Quotient : constant Long_Long_Float := Numerator / Denominator;
      N        : constant Integer         := Integer(Quotient - 0.5);

   begin
      return Numerator - Denominator * Long_Long_Float (N);
   end F_Mod;


   ---------------------------------------------------------------------------
   --
   -- Returns the inverse element K to T mod M  where
   -- T*K (mod M) = 1
   --
   ---------------------------------------------------------------------------
   function Get_Inverse(T : Large_Positive; M : Large_Positive) return Large_Positive is
   begin
      for K in 1 .. M-1 loop
         if ((T*K) mod M) = 1 then
            return K;
         end if;
      end loop;
      return 0;
   End Get_Inverse;


   ---------------------------------------------------------------------------
   -- Find elements from the Knapsack of n elements En where:
   --
   -- p <= p;
   --
   -- L := Ep + Ep-i ... + Ep-(i-k)
   --
   -- Returns the Element list as an index table marking the positions for the
   -- elements as boolean true values
   ---------------------------------------------------------------------------
   procedure Solve_Knapsack(Knapsack : in Large_Positive_Array_T; Index_Table : out Index_Array_T; L : in Large_Positive) is
      L_Tmp : Large_Positive := L;

   begin
      for I in reverse Knapsack'First .. Knapsack'Last loop
         if L_Tmp >= Knapsack(I) then
             Index_Table(I) := True;
             L_Tmp := L_Tmp - Knapsack(I);
         else
             Index_Table(I) := False;
         end if;
      end loop;
   end Solve_Knapsack;


   ---------------------------------------------------------------------------
   -- Convert an easy Knapsack and return a difficult knapsack and the keys
   -- K,M
   --
   ---------------------------------------------------------------------------
   procedure Convert_Easy_Knapsack_To_Difficult(Easy_Knapsack      : in Large_Positive_Array_T;
                                                Difficult_Knapsack : out Large_Positive_Array_T;
                                                K                  : out Large_Positive;
                                                M                  : out Large_Positive) is

      subtype Random_Interval is Large_Positive range Knapsack_Sum(Easy_Knapsack) .. (2 ** 15);
      package Random_Large_Positive is new Discrete_Random(Random_Interval);
      use Random_Large_Positive;

      G : Generator;

      M_Tmp  : constant Large_Positive := Random(G);
      T  : constant Large_Positive := Get_CoPrime(M_Tmp);

   begin
      M := M_Tmp;
      K := Get_Inverse(T,M);

      for Index in Easy_Knapsack'First .. Easy_Knapsack'Last loop
         Difficult_Knapsack(Index) := (Easy_Knapsack(Index) * T) mod M;
      end loop;

   end Convert_Easy_Knapsack_To_Difficult;



   ---------------------------------------------------------------------------
   --
   -- Returns the greatest common divisor for (A,B)
   --
   ---------------------------------------------------------------------------
   function Gcd(A : Large_Positive; B : Large_Positive) Return Large_Positive is
      A_Tmp : Large_Positive := A;
      B_Tmp : Large_Positive := B;
      Gcd   : Large_Positive := 1;
      R     : Large_Positive := 0;
   begin
     Euclid:
      loop
         R := A_Tmp mod B_Tmp;
         A_Tmp := B_Tmp;
         if R > 0 then
            B_Tmp := R;
         end if;
      exit Euclid when R = 0;
      end loop Euclid;
      Gcd := A_Tmp;
      return Gcd;
   end Gcd;

   ---------------------------------------------------------------------------
   --
   -- Returns True if A and B are co-primes => Gcd(A,B) = 1
   -- Otherwise returns False
   --
   ---------------------------------------------------------------------------
   function IsCoprime(A : Large_Positive; B : Large_Positive) return Boolean is
      Gcd_Val : constant Large_Positive := Gcd(A,B);
   begin
      if Gcd_Val > 1 then
         return False;
      else
         return True;
      end if;
   end IsCoprime;


   ---------------------------------------------------------------------------
   --
   -- Eulers Totient function
   -- Returns number of Positives Pi < N where Gcd(N,Pi) = 1
   --
   ---------------------------------------------------------------------------
   function Phi(N : Large_Positive) return Large_Positive is
      Phi_Val : Large_Positive := 1;
      Coprime : Boolean := False;
   begin
      for I in 2 .. N-1 loop
         Coprime := IsCoprime(I,N);

         if CoPrime then
            Phi_Val := Phi_Val +1;
         end if;
      end loop;
      return Phi_Val;
   end Phi;


   ---------------------------------------------------------------------------
   --
   -- Factorize N into primes. Prime factors are returned in the
   -- Prime_Factors array
   --
   ---------------------------------------------------------------------------
   procedure Factorize(N             : in  Large_Positive;
                       Prime_Factors : out Large_Positive_Array_T) is

      N_Tmp       : Large_Positive := N;
      Index       : Large_Positive := 1;
      Factor      : Large_Positive := 2;

   begin
      if N_Tmp < 2 then
         Prime_Factors(1) := 1;
      else
         while Factor < N_Tmp loop
            -- Is Factor a prime ?
            if (N_Tmp mod Factor) = 0 then
               Prime_Factors(Index) := Factor;
               N_Tmp := N_Tmp / Factor;
               Index := Index + 1;
            else
               if Factor = 2 then
                  Factor := 3;
               else
                  Factor := Factor + 2;
               end if;
            end if;
         end loop;
         Prime_Factors(Index) := Factor;
      end if;
   end Factorize;


   ---------------------------------------------------------------------------
   --
   -- Factor N in 2 * 2 .... * 2 * Factor
   --
   ---------------------------------------------------------------------------
   Procedure Factor_2_Powers(N : in Large_Positive;
                             Factor : out Large_Positive;
                             Two_Powers : out Large_Positive) is

     Gcd_Val : Large_Positive := Gcd(N,2);

   begin
      Factor     := N;
      Two_Powers := 0;

      while Gcd_Val /= 1 loop
         Two_Powers := Two_Powers + 1;
         Factor     := Factor / 2;
         Gcd_Val    := Gcd(Factor,2);
      end loop;
   end Factor_2_Powers;


   ---------------------------------------------------------------------------------------
   -- Reduce and calculate modulus expression with large Base and reduced prime exponent :
   -- (A ** B) (mod N)  where A = a0 * a1 * .. an =>
   -- [(a0 ** B) (mod N) * (a1 ** B) (mod N) .. * (an ** B) (mod N)] mod N
   ----------------------------------------------------------------------------------------

   function Reduce_Large_Base_Modulus(Base     : Large_Positive;
                                      Exponent : Large_Positive;
                                      Modulus  : Large_Positive) return Large_Positive is

      RetVal : Large_Positive := 1;

   begin
      for I in 1 .. Exponent loop
         RetVal := (RetVal * (Base mod Modulus)) mod Modulus;
      end loop;

      return RetVal;
   end Reduce_Large_Base_Modulus;


   ---------------------------------------------------------------------------
   -- Reduce and calculate modulus expression with large exponent:
   -- (A ** B) (mod N)  where B = b0 * b1 * .. bn =>
   -- [(A ** b0) (mod N)] ** b1 (mod N) ...
   --
   -- Unfortunally this functions only works for exponents with relative
   -- small prime factors. If the exponent is a big prime the function will
   -- overflow. It is more safe to use the straight and slow function:
   -- Slow_Reduce_Large_Exponent_Modulus
   ---------------------------------------------------------------------------
   function Reduce_Large_Exponent_Modulus(Base : Large_Positive;
                                          Exponent :  Natural;
                                          Modulus : Large_Positive) return Large_Positive is

      Exp                    : constant Large_Positive                 := Large_Positive(Exponent);
      Upper_Limit            : constant Large_Positive                 := Large_Positive(Exponent/2)+1;
      Exponent_Prime_Factors : Large_Positive_Array_T (1..Upper_Limit) := (others => 1);
      RetVal                 : Large_Positive                          := 1;

   begin
      Factorize(Exp,Exponent_Prime_Factors);

      RetVal := Reduce_Large_Base_Modulus(Base, Exponent_Prime_Factors(Exponent_Prime_Factors'First), Modulus);
      for I in Exponent_Prime_Factors'First +1  .. Exponent_Prime_Factors'Last loop
         exit when Exponent_Prime_Factors(I) = 1;
           RetVal := Reduce_Large_Base_Modulus(Base, Exponent_Prime_Factors(I), Modulus);
      end loop;
      return RetVal;
   end Reduce_Large_Exponent_Modulus;


   ---------------------------------------------------------------------------
   --
   -- Slow Reduce and calculate modulus expression with large exponent:
   --
   ---------------------------------------------------------------------------
   function Slow_Reduce_Large_Exponent_Modulus(Base     : Large_Positive;
                                               Exponent :  Natural;
                                               Modulus  : Large_Positive) return Large_Positive is

      RetVal : Large_Positive := Base;

   begin
      for I in 1 .. Exponent-1 loop
         RetVal := ((RetVal mod Modulus) * (Base mod Modulus))  mod Modulus;
      end loop;
      return RetVal;
   end Slow_Reduce_Large_Exponent_Modulus;



   ---------------------------------------------------------------------------------------
   -- Miller Rabin prime test:
   --
   -- Input: Prime_test :
   -- Test if:
   --    (X ** D) congruent to 1 (mod p)
   -- Or
   --    (X ** (2*R*D)) congruent to -1 (mod p)
   -- Where:
   --  0 < X < P
   --
   -- INPUT:
   --  Prime_test : Large_Positive odd number > 1 for prime test.
   --  Acuracy    : The acuracy for the prime test.
   --
   -- Returns true if Prime_test is a prime number with a probability > 1-(0.25 ** Acuracy)
   -------------------------------------------------------------------------------------
   function Miller_Rabin_Prime_Test(Prime_Test : Large_Positive; Acuracy : Large_Positive) return Boolean is

      subtype Random_Interval is Large_Positive range 2..(Prime_Test-2);
      package Random_Large_Positive is new Discrete_Random(Random_Interval);
      use Random_Large_Positive;

      G      : Generator;
      A      : Large_Positive;

      Factor : Natural  := 1;
      Powers : Large_Positive  := 0;
      X      : Large_Positive  := 0;

   begin
      -- Return if Prime_Test is not an odd number
      -- or if Prime_Test < 3
      if (Prime_Test < 3) or ((Prime_Test mod 2) = 0) then
         return False;
      end if;

      --  P-1 is written on the form ( 2 ** Powers) * Factor
      Factor_2_Powers(Prime_Test-1,Large_Positive(Factor), Powers);

      for I in 1 .. Acuracy loop
         A:=Random(G);
         if Factor > 1 then
            X := Reduce_Large_Exponent_Modulus(A,Factor,Prime_Test);
         else
            X := (A ** Factor) mod Prime_Test;
         end if;

         if (X /= 1) and (X /= Prime_Test -1) then
            Inner_Loop:
                for R in 1 .. Powers-1 loop
                   X := (X ** 2) mod Prime_Test;
                   exit Inner_Loop When X = Prime_Test -1;
                   if (X = 1)  then
                      -- Not a Prime number
                      return False;
                   end if;
                end loop Inner_Loop;

                if X /= Prime_Test -1 then
                   return False;
                end if;
         end if;
      end loop;

      -- Return True since Prime_test is proberly a prime number
      return True;
   end Miller_Rabin_Prime_Test;


   ---------------------------------------------------------------------------
   --
   -- Returns a prime number >= Lower_Limit
   --
   ---------------------------------------------------------------------------
   function Get_Prime(Lower_Limit : Large_Positive) return Large_Positive is
   begin
      for I in Lower_Limit .. Large_Positive'Last loop
         if Miller_Rabin_Prime_Test(I,20) then
            return I;
         end if;
      end loop;
      return 0;
   end Get_Prime;

   ---------------------------------------------------------------------------
   --
   -- Returns a prime number >= Lower_Limit
   --
   ---------------------------------------------------------------------------
   function Get_Prime_Slow(Lower_Limit : Large_Positive) return Large_Positive is
      P : Large_Positive := Lower_Limit;
      Prime : Boolean := True;

      Use Ada.Numerics.Elementary_Functions;
   begin
      loop
         Increment_Loop: Loop
            P := P + 1;
           exit Increment_Loop when P mod 2 /= 0;
         end loop Increment_loop;

         for I in 3 .. Large_Positive(Sqrt(Float(P))) loop
               if P mod I = 0 then
                  Prime := False;
               end if;
            exit when not Prime;
         end loop;
         if Prime then
            return P;
         end if;

         Prime := True;
      end loop;
   end Get_Prime_Slow;
end Number_Theory_Tools;
