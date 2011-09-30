-------------------------------------------------------------------------------
--                                                                           --
--                           Number Theory Tools                             --
--                                                                           --
--                         Number_Theory_Tools.ads                           --
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
package Number_Theory_Tools is

   type Large_Positive is mod 2 ** 48;

   type Large_Positive_Array_T is array (Large_Positive range <>) of Large_Positive;
   type Index_Array_T is array (Large_Positive range <>) of Boolean;


   ---------------------------------------------------------------------------
   --
   -- Returns a random number in the range First .. Last
   --
   ---------------------------------------------------------------------------
   function Get_Random_Large_Positive(First : Large_Positive; Last : Large_Positive)
                                     return Large_Positive;


   ---------------------------------------------------------------------------
   --
   -- Returns a modified second for use to get some more randomness in the
   -- Random function
   --
   ---------------------------------------------------------------------------
   function Get_A_Second(Random_Val : Large_Positive) return Large_Positive;


   ---------------------------------------------------------------------------
   --
   -- Returns the sum of the elements of the given Knapsack
   --
   ---------------------------------------------------------------------------
   function Knapsack_Sum(Knapsack : in Large_Positive_Array_T) return Large_Positive;

   ---------------------------------------------------------------------------
   --
   -- Generate and "Easy" Knapsack and returns it in the "Knapsack" buffer
   --
   ---------------------------------------------------------------------------
   procedure Generate_Easy_Knapsack(Knapsack : in out Large_Positive_Array_T);

   ---------------------------------------------------------------------------
   --
   -- Returns a coprime (the smallest) to A;
   --
   ---------------------------------------------------------------------------
   function Get_CoPrime(A : Large_Positive) return Large_Positive;

   ---------------------------------------------------------------------------
   --
   -- Returns the inverse element K to T mod M  where
   -- T*K (mod M) = 1
   --
   ---------------------------------------------------------------------------
   function Get_Inverse(T : Large_Positive; M : Large_Positive) return Large_Positive;

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
   procedure Solve_Knapsack(Knapsack : in Large_Positive_Array_T;
                            Index_Table : out Index_Array_T;
                            L : in Large_Positive);

   ---------------------------------------------------------------------------
   -- Convert an easy Knapsack and return a difficult knapsack and the keys
   -- K,M
   --
   ---------------------------------------------------------------------------
   procedure Convert_Easy_Knapsack_To_Difficult(Easy_Knapsack      : in Large_Positive_Array_T;
                                                Difficult_Knapsack : out Large_Positive_Array_T;
                                                K                  : out Large_Positive;
                                                M                  : out Large_Positive);


   ---------------------------------------------------------------------------
   --
   -- Returns the greatest common divisor for (A,B)
   --
   ---------------------------------------------------------------------------
   function Gcd(A : Large_Positive; B : Large_Positive) Return Large_Positive;


   ---------------------------------------------------------------------------
   --
   -- Returns True if A and B are co-primes => Gcd(A,B) = 1
   -- Otherwise returns False
   --
   ---------------------------------------------------------------------------
   function IsCoprime(A : Large_Positive; B : Large_Positive) return Boolean;


   ---------------------------------------------------------------------------
   --
   -- Eulers Totient function
   -- Returns number of Positives Pi < N where Gcd(N,Pi) = 1
   --
   ---------------------------------------------------------------------------
   function Phi(N : Large_Positive) return Large_Positive;


   ---------------------------------------------------------------------------
   --
   -- Factorize N into primes. Prime factors are returned in the
   -- Prime_Factors array
   --
   ---------------------------------------------------------------------------
   procedure Factorize(N             : in  Large_Positive;
                       Prime_Factors : out Large_Positive_Array_T);



   ---------------------------------------------------------------------------
   --
   -- Factor N in 2 * 2 .... * 2 * Factor
   --
   ---------------------------------------------------------------------------
   Procedure Factor_2_Powers(N : in Large_Positive;
                             Factor : out Large_Positive;
                             Two_Powers : out Large_Positive);


   ---------------------------------------------------------------------------------------
   -- Reduce and calculate modulus expression with large Base and reduced prime exponent :
   -- (A ** B) (mod N)  where A = a0 * a1 * .. an =>
   -- [(a0 ** B) (mod N) * (a1 ** B) (mod N) .. * (an ** B) (mod N)] mod N
   ----------------------------------------------------------------------------------------

   function Reduce_Large_Base_Modulus(Base : Large_Positive;
                                      Exponent :  Large_Positive;
                                      Modulus : Large_Positive) return Large_Positive;



   ---------------------------------------------------------------------------
   -- Reduce and calculate modulus expression with large exponent:
   -- (A ** B) (mod N)  where B = b0 * b1 * .. bn =>
   -- [(A ** b0) (mod N)] ** b1 (mod N) ...
   ---------------------------------------------------------------------------
   function Reduce_Large_Exponent_Modulus(Base : Large_Positive;
                                          Exponent :  Natural;
                                          Modulus : Large_Positive) return Large_Positive;



   ---------------------------------------------------------------------------
   --
   -- Slow Reduce and calculate modulus expression with large exponent:
   --
   ---------------------------------------------------------------------------
   function Slow_Reduce_Large_Exponent_Modulus(Base     : Large_Positive;
                                               Exponent :  Natural;
                                               Modulus  : Large_Positive) return Large_Positive;


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
   -- Acuracy     : The acuracy for the prime test.
   --
   -- Returns true if Prime_test is a prime number with a probability > 1-(0.25 ** Acuracy)
   -------------------------------------------------------------------------------------
   function Miller_Rabin_Prime_Test(Prime_Test : Large_Positive; Acuracy : Large_Positive) return Boolean;


   ---------------------------------------------------------------------------
   --
   -- Returns a prime number >= Lower_Limit
   --
   ---------------------------------------------------------------------------
   function Get_Prime(Lower_Limit : Large_Positive) return Large_Positive ;
end Number_Theory_Tools;
