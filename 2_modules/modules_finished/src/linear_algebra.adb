with Ada.Text_IO; use Ada.Text_IO;

package body Linear_Algebra is

   procedure Display_Matrix (A : Matrix) is
   begin
      for I in 1 .. 3 loop
         for J in 1 .. 3 loop
            Put (Img (A (I, J)) & " ");
         end loop;
         New_Line;
      end loop;
      New_Line;
   end Display_Matrix;

   procedure Set_Matrix (
      A : in out Matrix;
      I : Integer;
      J : Integer;
      V : Integer) is
   begin
      A (I, J) := V;
   end Set_Matrix;

   function Trace (A : Matrix) return Integer is
      T : Integer := 0;
   begin
      for I in 1 .. 3 loop
         T := T + A (I, I);
      end loop;
      return T;
   end Trace;

end Linear_Algebra;