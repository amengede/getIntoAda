with Ada.Text_IO;

procedure Functions is
   use Ada.Text_IO;

   type Matrix is array (1 .. 3, 1 .. 3) of Integer;

   My_Matrix : Matrix :=
   ((1, 2, 3),
   (4, 5, 6),
   (7, 8, 9));

   procedure Display_Matrix (A : Matrix) is
   begin
      for I in 1 .. 3 loop
         for J in 1 .. 3 loop
            Put (A (I, J)'Image & " ");
         end loop;
         Put_Line ("");
      end loop;
      Put_Line ("");
   end Display_Matrix;

   procedure Say_Hello is
   begin
      Put_Line ("Hello, world!");
   end Say_Hello;

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

begin
   Say_Hello;
   Display_Matrix (My_Matrix);
   Set_Matrix (My_Matrix, 1, 1, 12);
   Display_Matrix (My_Matrix);
   Put_Line ("The trace of my matrix is " & Trace (My_Matrix)'Image);

end Functions;
