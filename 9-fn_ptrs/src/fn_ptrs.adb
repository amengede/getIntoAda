with Ada.Text_IO; use Ada.Text_IO;

procedure Fn_Ptrs is

    procedure Say_Hello is
    begin
        Put_Line ("Hello, world!");
    end Say_Hello;

    type proc_ptr is access procedure;

    Speak : constant proc_ptr := Say_Hello'Access;

    function Add (A, B : Integer) return Integer is
    begin
        return A + B;
    end Add;

    type fn_ptr is access function (A, B : Integer) return Integer;
    Math_Op : constant fn_ptr := Add'Access;
    Result : Integer;

    type StateType is (
        Add,
        Multiply,
        NoChange);

    type update_fn is access function (A, B : Integer) return StateType;

    type State is
        record
            Enter, Leave : proc_ptr := null;
            Update : update_fn := null;
        end record;

    type State_Array is array (StateType) of State;

    procedure Enter_Add is
    begin
        Put_Line ("Ok, let's start adding now!");
    end Enter_Add;

    function Update_Add (A, B : Integer) return StateType is
        Result : constant Integer := A + B;
    begin
        Put_Line (A'Image & " +" & B'Image & "= " & Result'Image);
        return Multiply;
    end Update_Add;

    procedure Leave_Add is
    begin
        Put_Line ("Ok, let's stop adding now!");
    end Leave_Add;

    procedure Enter_Multiply is
    begin
        Put_Line ("Ok, let's start multiplying now!");
    end Enter_Multiply;

    function Update_Multiply (A, B : Integer) return StateType is
        Result : constant Integer := A * B;
    begin
        Put_Line (A'Image & " *" & B'Image & "= " & Result'Image);
        return NoChange;
    end Update_Multiply;

    procedure Leave_Multiply is
    begin
        Put_Line ("Ok, let's stop multiplying now!");
    end Leave_Multiply;

    States : State_Array;
    Current_State, Next_State : StateType := Add;

begin
    Speak.all;

    Result := Math_Op.all (2, 4);
    Put_Line ("4 + 2 =" & Result'Image);

    States (Add) := State' (
        Enter => Enter_Add'Access,
        Update => Update_Add'Access,
        Leave => Leave_Add'Access);

    States (Multiply) := State' (
        Enter => Enter_Multiply'Access,
        Update => Update_Multiply'Access,
        Leave => Leave_Multiply'Access);

    States (Current_State).Enter.all;

    for I in 0 .. 2 loop
        Next_State := States (Current_State).Update.all (2, 4);

        case Next_State is
            when NoChange =>
                null;
            when others =>
                States (Current_State).Leave.all;
                Current_State := Next_State;
                States (Next_State).Enter.all;
        end case;
    end loop;

    States (Current_State).Leave.all;
end Fn_Ptrs;
