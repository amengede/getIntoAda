with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

procedure Split_Stuff is
   Line: Unbounded_String;

   package String_List
      is new Ada.Containers.Vectors (
         Index_Type => Natural,
         Element_Type => Unbounded_String);
   
   Words : String_List.Vector;

   function Split (
      Line : Unbounded_String;
      Delimeter : String
   ) return String_List.Vector is

      I : Positive := 1;
      Pos : Natural;
      List : String_List.Vector;
   begin
      while I < Length (Line) loop
         Pos := Index (Line, Delimeter, I);
         exit when Pos = 0;

         List.Append (Unbounded_Slice (line, I, Pos - 1));
         I := Pos + Length (To_Unbounded_String (Delimeter));
      end loop;

      List.Append (Unbounded_Slice (line, I, Length (Line)));

      return List;
   end Split;

begin
   
   Put ("Write some input: ");
   Line := To_Unbounded_String (Get_Line);
   Put_Line ("You wrote " & To_String (Line));

   Words := Split (Line, " ");

   Put_Line ("Words:");
   for Word of Words loop
      Put_Line (To_String(Word));
   end loop;


end Split_Stuff;
