with Ada.Text_IO;
with Linear_Algebra;

procedure Modules_Finished is
   use Ada.Text_IO;
   use Linear_Algebra;

   My_Matrix : Matrix :=
   ((1, 2, 3),
   (4, 5, 6),
   (7, 8, 9));

begin
   Display_Matrix (Identity);
   Set_Matrix (My_Matrix, 1, 1, 12);
   Display_Matrix (My_Matrix);
   Put_Line ("The trace of my matrix is " & Trace (My_Matrix)'Image);

end Modules_Finished;
