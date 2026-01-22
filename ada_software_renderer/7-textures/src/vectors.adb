with Ada.Unchecked_Deallocation;

package body Vectors is

    procedure Free is new Ada.Unchecked_Deallocation
                              (Object => Elements_Type, Name => Elements_Access);

    procedure Resize (Target: Vector_Access) is
        New_Elements: Elements_Access := new Elements_Type (0 .. 2 * Target.Capacity);
        Old_Elements: Elements_Access := Target.Elements;
    begin
        New_Elements (0 .. Target.Capacity) := Old_Elements (0 .. Target.Capacity);
        Free (Old_Elements);
        Target.Elements := New_Elements;
        Target.Capacity := Target.Capacity * 2;
    end Resize;

    procedure Insert (Target: Vector_Access; Index: Natural; Element: T) is
    begin

        while (Index >= Target.Capacity) loop
            Resize (Target);
        end loop;

        Target.Elements (Index) := Element;

    end Insert;

    procedure Append (Target: Vector_Access; Element: T) is
    begin

        if (Target.Size >= Target.Capacity) then
            Resize (Target);
        end if;

        Target.Elements (Target.Size) := Element;
        Target.Size := Target.Size + 1;

    end Append;

    function Get (Target: Vector_Access; Index: Natural) return T is
    begin
        return Target.Elements (Index);
    end Get;

    procedure Reserve (Target : Vector_Access;
                      New_Capacity : Natural) is
        New_Elements: Elements_Access := new Elements_Type (0 .. New_Capacity);
        Old_Elements: Elements_Access := Target.Elements;
    begin
        New_Elements (0 .. Target.Capacity) := Old_Elements (0 .. Target.Capacity);
        Free (Old_Elements);
        Target.Elements := New_Elements;
        Target.Capacity := New_Capacity;
    end Reserve;

end Vectors;
