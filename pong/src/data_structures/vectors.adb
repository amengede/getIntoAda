with Ada.Unchecked_Deallocation;

package body Vectors is

    procedure Push_Back (Self : in out Vector; Element : T) is
    begin
        if Self.Data = null then
            Self.Data := new Internal_Vector;
        end if;

        while Self.Data.Size >= Self.Data.Capacity loop
            Self.Reserve (2 * Self.Data.Capacity);
        end loop;

        Self.Data.Elements (Self.Data.Size) := Element;
        Self.Data.Size := Self.Data.Size + 1;

    end Push_Back;

    function Get (Self : Vector; Index : Natural) return T is
    begin

        if Self.Data = null then
            raise Uninitialized_Exception with
                "Trying to access empty vector";
        end if;

        if Index >= Self.Data.Size then
            raise Out_Of_Bounds_Exception with
                "Accessing beyond end of vector";
        end if;

        return Self.Data.Elements (Index);
    end Get;

    procedure Free is new Ada.Unchecked_Deallocation
                                (Object => Elements_Type,
                                Name => Elements_Access);

    procedure Free is new Ada.Unchecked_Deallocation
                            (Object => Internal_Vector,
                             Name => Internal_Vector_Ptr);

    procedure Reserve (Self : in out Vector; New_Capacity : Natural) is
        New_Elements : Elements_Access;
    begin

        if Self.Data = null then
            Self.Data := new Internal_Vector;
        end if;

        if New_Capacity <= Self.Data.Capacity then
            return;
        end if;

        New_Elements := new Elements_Type (0 .. New_Capacity - 1);

        New_Elements (0 .. Self.Data.Capacity - 1) :=
            Self.Data.Elements (0 .. Self.Data.Capacity - 1);
        Free (Self.Data.Elements);
        Self.Data.Elements := New_Elements;
        Self.Data.Capacity := New_Capacity;
    end Reserve;

    procedure Resize (Self : in out Vector; New_Size : Natural) is
        New_Elements : Elements_Access;
    begin
        if Self.Data = null then
            Self.Data := new Internal_Vector;
        end if;

        if New_Size <= Self.Data.Capacity then
            return;
        end if;

        New_Elements := new Elements_Type (0 .. New_Size - 1);

        New_Elements (0 .. Self.Data.Capacity - 1) :=
            Self.Data.Elements (0 .. Self.Data.Capacity - 1);
        Free (Self.Data.Elements);
        Self.Data.Elements := New_Elements;
        Self.Data.Capacity := New_Size;
        Self.Data.Size := New_Size;
    end Resize;

    procedure Erase (Self : in out Vector; Index : Natural) is
    begin
        if Self.Data = null then
            raise Uninitialized_Exception with
                "Trying to erase from empty vector";
        end if;

        if Index >= Self.Data.Size then
            raise Out_Of_Bounds_Exception with
                "Erasing beyond end of vector";
        end if;

        Self.Data.Elements (Index) :=
            Self.Data.Elements (Self.Data.Size - 1);
        Self.Data.Size := Self.Data.Size - 1;
    end Erase;

    procedure Clear (Self : in out Vector) is
    begin
        if Self.Data = null then
            raise Uninitialized_Exception with
                "Trying to clear empty vector";
        end if;

        Self.Data.Size := 0;
    end Clear;

    procedure Shrink_To_Fit (Self : in out Vector) is
        New_Elements : Elements_Access;
    begin
        if Self.Data = null then
            raise Uninitialized_Exception with
                "Trying to access empty vector";
        end if;

        New_Elements := new Elements_Type (0 .. Self.Data.Size - 1);

        New_Elements (0 .. Self.Data.Size - 1) :=
            Self.Data.Elements (0 .. Self.Data.Size - 1);
        Free (Self.Data.Elements);
        Self.Data.Elements := New_Elements;
        Self.Data.Capacity := Self.Data.Size;
    end Shrink_To_Fit;

    function Get_Capacity (Self : Vector) return Natural is
    begin
        if Self.Data = null then
            return 0;
        end if;

        return Self.Data.Capacity;
    end Get_Capacity;

    function Get_Size (Self : Vector) return Natural is
    begin
        if Self.Data = null then
            return 0;
        end if;

        return Self.Data.Size;
    end Get_Size;

    overriding
    procedure Adjust (Self : in out Vector) is
    begin
        if Self.Data = null then
            return;
        end if;

        Self.Data.Reference_Count := Self.Data.Reference_Count + 1;
    end Adjust;

    overriding
    procedure Finalize (Self : in out Vector) is
    begin
        if Self.Data = null then
            return;
        end if;

        Self.Data.Reference_Count := Self.Data.Reference_Count - 1;

        if Self.Data.Reference_Count = 0 then
            Free (Self.Data.Elements);
            Free (Self.Data);
            Self.Data := null;
        end if;
    end Finalize;

end Vectors;
