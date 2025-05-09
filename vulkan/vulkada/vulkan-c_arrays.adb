-- This file is part of VulkAda.

-- VulkAda is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Lesser General Public License as
-- published by the Free Software Foundation, either version 3 of
-- the License, or (at your option) any later version.

-- VulkAda is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Lesser General Public License for more details.

-- You should have received a copy of the GNU Lesser General Public
-- License along with VulkAda.
-- If not, see <http://www.gnu.org/licenses/>.

-- Copyright 2025 Phaser Cat Games LLC

-- Generic package for handling C arrays

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Unchecked_Deallocation;

package body Vulkan.C_Arrays is
    type Allocation_Record is
    record
        Pointer: C_Arrays.Pointer;
        Array_Access: C_Arrays.Array_Access;
    end record;

    function "=" (Left, Right: in Allocation_Record) return Boolean;

    procedure Free is new Ada.Unchecked_Deallocation(Element_Array,
                                                     Array_Access);

    package Allocation_Lists is
        new Ada.Containers.Doubly_Linked_Lists(Allocation_Record);

    protected Allocations is
        procedure Add(Allocation: in Allocation_Record);
        procedure Free(Pointer: in C_Arrays.Pointer);
        procedure Free(Pointer: in C_Arrays.Pointer;
                       Release_Element: in Free_Element);

    private
        Records: Allocation_Lists.List;
    end Allocations;

    function Allocate(Size: in Positive) return Array_Access is
        Allocation: Allocation_Record;
    begin
        Allocation.Array_Access := new Element_Array(1 .. Size);
        Allocation.Pointer := Allocation.Array_Access(1)'Access;
        Allocations.Add(Allocation);

        return Allocation.Array_Access;
    end Allocate;

    procedure Free(P: in out Pointer) is
    begin
        if P /= null then
            Allocations.Free(P);
            P := null;
        end if;
    end Free;

    procedure Free(P: in out Pointer; Release_Element: in Free_Element) is
    begin
        if P /= null then
            Allocations.Free(P, Release_Element);
            P := null;
        end if;
    end Free;

    function "=" (Left, Right: in Allocation_Record) return Boolean is
    begin
        return Left.Pointer = Right.Pointer;
    end "=";

    protected body Allocations is
        procedure Add(Allocation: in Allocation_Record) is
        begin
            Records.Append(Allocation);
        end Add;

        procedure Free(Pointer: in C_Arrays.Pointer) is
            procedure Release(Element: in out Allocation_Record) is
            begin
                Free(Element.Array_Access);
            end Release;

            Dummy_Record: Allocation_Record;
            Position: Allocation_Lists.Cursor;
        begin
            Dummy_Record.Pointer := Pointer;
            Position := Records.Find(Dummy_Record);
            Records.Update_Element(Position, Release'Access);
            Records.Delete(Position);
        end Free;

        procedure Free(Pointer: in C_Arrays.Pointer;
                       Release_Element: in Free_Element) is
            procedure Release(Element: in out Allocation_Record) is
            begin
                for Item of Element.Array_Access.all loop
                    Release_Element(Item);
                end loop;

                Free(Element.Array_Access);
            end Release;

            Dummy_Record: Allocation_Record;
            Position: Allocation_Lists.Cursor;
        begin
            Dummy_Record.Pointer := Pointer;
            Position := Records.Find(Dummy_Record);
            Records.Update_Element(Position, Release'Access);
            Records.Delete(Position);
        end Free;
    end Allocations;
end Vulkan.C_Arrays;

